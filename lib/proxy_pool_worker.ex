require Lager

defmodule ProxyLists do
  defstruct avaliable: %{}
end

defmodule ProxyPoolWorker do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, :ok, [name: :proxy_pool])
  end

  def init(_) do
    :erlang.process_flag(:trap_exit, true)
    Process.send_after(:proxy_pool, :init_store, 1)
    {:ok, nil}
  end

  # get random avaliable proxy
  def random(source) do
    GenServer.call :proxy_pool, {:random, source}
  end

  # proxy cannot work
  def fail_notice(source, proxy) do
    GenServer.cast :proxy_pool, {:fail_notice, source, proxy}
  end

  # update the proxy list api
  def update do
    GenServer.cast :proxy_pool, :update
  end

  def handle_call({:random, source}, _from, nil) do
    state = %ProxyLists{}
    {:reply, nil, state}
  end

  def handle_call({:random, source}, _from, %ProxyLists{avaliable: avaliable_list}=state) do
    # the situation that proxy been move from avaliable_list to invalid_list all
    source_data = avaliable_list[source]
    if is_nil(source_data) || tuple_size(source_data[:proxys]) <= 0 do
      random_proxy = nil
    else
      index = source_data[:index]
      count = tuple_size source_data[:proxys]
      tail = rem(index, count)

      random_proxy = elem(source_data[:proxys], tail)
      new_avaliable_list = Dict.put avaliable_list, source,
        %{index: index+1, proxys: source_data[:proxys]}
      state = %ProxyLists{avaliable: new_avaliable_list}
    end

    {:reply, random_proxy, state}
  end

  def handle_cast({:fail_notice, source, proxy}, %ProxyLists{avaliable: avaliable_list}=state) do
    # move the proxy which had problem from avaliable_list o invalid_list
    source_data = avaliable_list[source]
    unless is_nil(source_data) do
      new_proxys = ((source_data[:proxys] |> Tuple.to_list) -- [proxy]) |> List.to_tuple
      # update proxy list
      new_avaliable_list = Dict.put avaliable_list, source,
        %{index: source_data[:index], proxys: new_proxys}

      state = %ProxyLists{avaliable: new_avaliable_list}
      # call the check_invalid server
      CheckInvalidWorker.push(source, proxy)
    end

    {:noreply, state}
  end

  def handle_cast(:update, state) do
    case query_proxy_pool do
      {:ok, result} when is_nil(result) ->
        new_state = state
      {:ok, result} ->
        new_state = result
        # if Set.disjoint?(result.avaliable, invalid_list) do
        #   new_state = %ProxyLists{avaliable: result.avaliable, invalid: invalid_list}
        # else
        #   common_invalid_proxys = Set.intersection(result.avaliable, invalid_list) |> HashSet.to_list
        #   new_avaliable_list = ((result.avaliable |> HashSet.to_list) -- common_invalid_proxys)
        #     |> Enum.into(HashSet.new)
        #   # invalid_list should continue to deal with check queue
        #   new_state = %ProxyLists{avaliable: new_avaliable_list, invalid: invalid_list}
        # end
    end

    {:noreply, new_state}
  end

  def handle_info(:init_store, state) do
    case query_proxy_pool do
      {:ok, result} when is_nil(result) ->
        new_state = nil
      {:ok, result} ->
        new_state = result
    end

    {:noreply, new_state}
  end

  def check_invalid_callback(result, source, proxy) do
    GenServer.cast :proxy_pool, {:check_invalid_callback, result, source, proxy}
  end

  def handle_cast({:check_invalid_callback, result, source, proxy}, %ProxyLists{avaliable: avaliable_list}=state) do
    # Lager.info "invalid_list -> #{Set.size(invalid_list)}"

    case result do
      "success" ->
        source_data = avaliable_list[source]
        new_proxys = Tuple.append(source_data[:proxys], proxy)
          |> Tuple.to_list
          |> Enum.uniq
          |> List.to_tuple

        new_avaliable_list = Dict.put avaliable_list, source,
          %{index: source_data[:index], proxys: new_proxys}
        # update proxy list
        new_state = %ProxyLists{avaliable: new_avaliable_list}
      "fail" ->
        # retry 3 times to ensure the proxy cannot work,
        # so remove this proxy from invalid_list
        # TODO remove proxy from ssdb
        # new_invalid_list = Set.delete invalid_list, proxy
        new_state = state
    end

    {:noreply, new_state}
  end

  def query_proxy_pool do
    state = try do
      case SSDB.query ["hgetall", Application.get_env(:proxy_pool, :ssdb_key)] do
        ["ok"] ->
          nil
        ["ok" | result]->
          avaliable_list = result |> Stream.chunk(2) |> Enum.reduce(%{}, fn(x, acc) ->
            [source_key | [proxys]] = x
            parsed_proxys = proxys |> Poison.decode! |> Enum.uniq |> List.to_tuple
            Dict.put acc, String.to_atom(source_key), %{proxys: parsed_proxys, index: 0}
          end)

          %ProxyLists{avaliable: avaliable_list}
        reason when is_tuple(reason) or is_list(reason) ->
          Lager.error "qrange from ssdb error ~s", reason
          nil
      end
    catch
      :exit, reason ->
        Lager.error("fail to connect ssdb, 5s later retry! reason -> #{inspect reason}")
        Process.send_after(:proxy_pool, :init_store, 5000)

      nil
    end

    {:ok, state}
  end

end

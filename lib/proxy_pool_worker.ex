require Lager

defmodule ProxyLists do
  defstruct avaliable: HashSet.new, invalid: HashSet.new
end

defmodule ProxyPoolWorker do
  use GenServer
  @interval 3000
  @retry_time 3
  @test_host "http://www.baidu.com"

  def start_link do
    GenServer.start_link(__MODULE__, :ok, [name: :proxy_pool])
  end

  def init(_) do
    Process.send_after(:proxy_pool, :check_invalid_list, @interval)
    {:ok, _} = query_proxy_pool
  end

  # get random avaliable proxy
  def random do
    GenServer.call :proxy_pool, :random
  end

  # proxy cannot work
  def fail_notice(proxy) do
    GenServer.cast :proxy_pool, {:fail_notice, proxy}
  end

  # update the proxy list api
  def update do
    GenServer.cast :proxy_pool, :update
  end

  def handle_call(:random, _from, nil) do
    state = %ProxyLists{}
    {:reply, nil, state}
  end

  def handle_call(:random, _from, %ProxyLists{avaliable: avaliable_list, invalid: _}=state) do
    # the situation that proxy been move from avaliable_list to invalid_list all
    if  Set.size(avaliable_list) <= 0 do
      random_proxy = nil
    else
      random_proxy = avaliable_list |> Enum.random
    end

    {:reply, random_proxy, state}
  end

  def handle_cast({:fail_notice, proxy}, %ProxyLists{avaliable: avaliable_list, invalid: invalid_list}=_state) do
    # move the proxy which had problem from avaliable_list o invalid_list
    new_avaliable_list = Set.delete avaliable_list, proxy
    new_invalid_list = Set.put invalid_list, proxy
    # update proxy list
    new_state = %ProxyLists{avaliable: new_avaliable_list, invalid: new_invalid_list}

    {:noreply, new_state}
  end

  def handle_cast(:update, %ProxyLists{avaliable: _, invalid: invalid_list}=state) do
    case query_proxy_pool do
      {:ok, result} when is_nil(result) ->
        new_state = state
      {:ok, result} ->
        if Set.disjoint?(result.avaliable, invalid_list) do
          new_state = %ProxyLists{avaliable: result.avaliable, invalid: invalid_list}
        else
          common_invalid_proxys = Set.intersection(result.avaliable, invalid_list) |> HashSet.to_list
          new_avaliable_list = ((result.avaliable |> HashSet.to_list) -- common_invalid_proxys)
            |> Enum.into(HashSet.new)
          # invalid_list should continue to deal with check queue
          new_state = %ProxyLists{avaliable: new_avaliable_list, invalid: invalid_list}
        end
    end

    {:noreply, new_state}
  end

  def handle_info(:check_invalid_list, nil) do
    {:noreply, nil}
  end

  def handle_info(:check_invalid_list, %ProxyLists{avaliable: avaliable_list, invalid: invalid_list}=state) do
    # Lager.info "invalid_list #{inspect invalid_list}"

    invalid_list |> Set.to_list |> Enum.map fn p ->
      spawn_link(fn -> check_single_invalid(p, @retry_time) end)
    end

    if Set.size(invalid_list) == 0 do
      Process.send_after(:proxy_pool, :check_invalid_list, @interval)
    else
      new_invalid_list = HashSet.new
      state = %ProxyLists{avaliable: avaliable_list, invalid: new_invalid_list}
    end

    {:noreply, state}
  end

  def handle_info({:check_invalid_callback, result, proxy}, %ProxyLists{avaliable: avaliable_list, invalid: invalid_list}=state) do
    # Lager.info "invalid_list -> #{Set.size(invalid_list)}"

    case result do
      "success" ->
        # new_invalid_list = Set.delete invalid_list, proxy
        new_avaliable_list = Set.put avaliable_list, proxy
        # update proxy list
        new_state = %ProxyLists{avaliable: new_avaliable_list, invalid: invalid_list}
      "fail" ->
        # retry 3 times to ensure the proxy cannot work,
        # so remove this proxy from invalid_list
        # TODO remove proxy from ssdb
        # new_invalid_list = Set.delete invalid_list, proxy
        new_state = state
    end

    # will have problem when multiple call arrived
    Process.send_after(:proxy_pool, :check_invalid_list, @interval)

    {:noreply, new_state}
  end

  def check_single_invalid(proxy, retry_time) when retry_time > 0 do
    Lager.info "check_single_invalid retry_time #{inspect retry_time}"
    case HTTPoison.request(:get, @test_host, "", [],
      [recv_timeout: 3000, connect_timeout: 2000, proxy: proxy]
    ) do
        {:ok, %HTTPoison.Response{status_code: code, body: _body, headers: _headers}} ->
          case code do
            200 ->
              # callback check_invalid_list
              send :proxy_pool, {:check_invalid_callback, "success", proxy}
            _ ->
              check_single_invalid(proxy, (retry_time - 1))
          end
        {:error, %HTTPoison.Error{reason: reason}} ->
          Lager.error "check_invalid error #{inspect proxy} -> reason#{inspect reason}"
          check_single_invalid(proxy, (retry_time - 1))
    end
  end

  def check_single_invalid(proxy, 0) do
    Lager.info "check_single_invalid proxy -> #{inspect proxy} fail"
    # callback check_invalid_list
    send :proxy_pool, {:check_invalid_callback, "fail", proxy}
  end

  def query_proxy_pool do
    case SSDB.query ["qrange", Application.get_env(:proxy_pool, :ssdb_key), "", ""] do
      ["ok"] ->
        state = nil
      ["ok" | result]->
        avaliable_list =  result |> Enum.uniq |> Enum.into(HashSet.new)
        state = %ProxyLists{avaliable: avaliable_list}
      reason when is_tuple(reason) or is_list(reason) ->
        Lager.error "qrange from ssdb error ~s", reason
        state = nil
    end

    {:ok, state}
  end

end

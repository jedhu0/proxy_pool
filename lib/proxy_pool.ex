require Lager
require IEx

defmodule ProxyLists do
  defstruct avaliable: HashSet.new, invalid: HashSet.new
end

defmodule ProxyPool do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, :ok, [name: :proxy_pool])
  end

  def init(_) do
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
    {:reply, :no_proxy_data, state}
  end

  def handle_call(:random, _from, %ProxyLists{avaliable: avaliable_list, invalid: _}=state) do
    if  Set.size(avaliable_list) <= 0 do
      random_proxy = :no_proxy_data
    else
      random_proxy = avaliable_list |> Enum.random
    end

    {:reply, random_proxy, state}
  end

  def handle_cast({:fail_notice, proxy}, %ProxyLists{avaliable: avaliable_list, invalid: invalid_list}=state) do
    new_avaliable_list = Set.delete avaliable_list, proxy
    new_invalid_list = Set.put invalid_list, proxy
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

  def query_proxy_pool do
    case SSDB.query ["qrange", Application.get_env(:ssdb, :proxy_pool_key), "", ""] do
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

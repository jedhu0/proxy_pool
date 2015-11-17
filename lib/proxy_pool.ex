require Lager

defmodule ProxyLists do
  defstruct avaliable: HashSet.new, invalid: HashSet.new
end

defmodule ProxyPool do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, :ok, [name: :proxy_pool])
  end

  def init(_) do
    {:ok, state} = query_proxy_pool
  end

  # get random avaliable proxy
  def get_one do
    GenServer.call :proxy_pool, :get_one
  end

  # proxy cannot work
  def fail_notice(proxy) do
    GenServer.cast :proxy_pool, {:fail_notice, proxy}
  end

  def handle_call(:get_one, _from, nil) do
    {:reply, :no_proxy_data, nil}
  end

  def handle_call(:get_one, _from, %ProxyLists{avaliable: avaliable_list, invalid: invalid_list}=state) do
    random_proxy = avaliable_list |> Enum.random
    {:reply, random_proxy, %ProxyLists{avaliable: avaliable_list, invalid: invalid_list}}
  end

  def handle_cast({:fail_notice, proxy}, %ProxyLists{avaliable: avaliable_list, invalid: invalid_list}=state) do
    new_avaliable_list = Set.delete avaliable_list, proxy
    new_invalid_list = Set.put invalid_list, proxy
    {:noreply, %ProxyLists{avaliable: new_avaliable_list, invalid: new_invalid_list}}
  end

  def query_proxy_pool do
    case SSDB.query ["qrange", Application.get_env(:ssdb, :proxy_pool_key), "", ""] do
      ["ok"] ->
        state = nil
      ["ok" | result]->
        state = %ProxyLists{avaliable: Enum.into(result, HashSet.new)}
      reason when is_tuple(reason) or is_list(reason) ->
        Lager.error "qrange from ssdb error ~s", reason
        state = nil
    end

    {:ok, state}
  end
end

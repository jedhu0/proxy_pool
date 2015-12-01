require Lager

defmodule CheckInvalidWorker do
  use GenServer
  @retry_time 3

  def start_link do
    GenServer.start_link(__MODULE__, :ok, [name: :check_invalid])
  end

  def push(source, invalid_proxy) do
    GenServer.cast :check_invalid, {:check, source, invalid_proxy}
  end

  def handle_info({:check, source, nil}, state) do
    {:noreply, state}
  end

  def handle_cast({:check, source, invalid_proxy}, state) do
    spawn_link(fn -> check_single_invalid(source, invalid_proxy, @retry_time) end)
    {:noreply, state}
  end

  def check_single_invalid(source, proxy, retry_time) when retry_time > 0 do
    # Lager.info "check_single_invalid retry_time #{inspect retry_time}"
    url = Application.get_env(:proxy_pool, :test_source)[source]

    if is_nil(url) do
      Lager.error "check_single_invalid source error -> #{inspect source}"
    else
      case HTTPoison.request(:get, url, "", [],
        [recv_timeout: 3000, connect_timeout: 2000, proxy: proxy]
      ) do
          {:ok, %HTTPoison.Response{status_code: code, body: _body, headers: _headers}} ->
            case code do
              200 ->
                # callback check_invalid_list
                ProxyPoolWorker.check_invalid_callback("success", source, proxy)
              _ ->
                check_single_invalid(source, proxy, (retry_time - 1))
            end
          {:error, %HTTPoison.Error{reason: reason}} ->
            # Lager.error "check_invalid error #{inspect proxy} -> reason#{inspect reason}"
            check_single_invalid(source, proxy, (retry_time - 1))
      end
    end
  end

  def check_single_invalid(source, proxy, 0) do
    Lager.info "check_single_invalid proxy -> #{inspect proxy} fail"
    # callback check_invalid_list
    # send :proxy_pool, {:check_invalid_callback, "fail", source, proxy}
  end
end

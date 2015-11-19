defmodule ProxyPool do
  use Application

  def start(_type, _args) do
    {:ok, _} = ProxyPoolSup.start_link
  end

  def random do
    ProxyPoolWorker.random
  end

  def fail_notice(proxy) do
    ProxyPoolWorker.fail_notice proxy
  end

  def update do
    ProxyPoolWorker.update
  end
end

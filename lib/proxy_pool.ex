defmodule ProxyPool do
  use Application

  def start(_type, _args) do
    CheckInvalidSup.start_link
    {:ok, _} = ProxyPoolSup.start_link
  end

  def random(source) do
    ProxyPoolWorker.random(source)
  end

  def fail_notice(source, proxy) do
    ProxyPoolWorker.fail_notice source, proxy
  end

  def update do
    ProxyPoolWorker.update
  end
end

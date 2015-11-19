defmodule ProxyPoolSup do
  use Supervisor

  def start_link(opts \\ [name: :proxy_pool_sup]) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init(_conf) do
    children = [worker(ProxyPoolWorker, [])]

    supervise(children, strategy: :one_for_one)
  end
end

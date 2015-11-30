defmodule CheckInvalidSup do
  use Supervisor

  def start_link(opts \\ [name: :check_invalid_sup]) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def init(_conf) do
    children = [worker(CheckInvalidWorker, [])]

    supervise(children, strategy: :one_for_one)
  end
end

defmodule NewrelicElx.Sup do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    children = [
      worker(NewrelicElx.Poller, [fn -> :newrelic_stats_transformer.poll end])
    ]
    supervise(children, strategy: :one_for_one)
  end
end

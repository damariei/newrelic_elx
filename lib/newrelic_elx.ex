require Logger

defmodule NewrelicElx do
  @moduledoc """
  Provides a statman instance and recording functions to monitor stats and errors
  with NewRelic.
  """

  use Application

  @doc """
  Starts staman process, adds listener to send recorded stats to NewRelic
  """
  def start(_type, _args) do
    NewrelicElx.Sup.start_link()
    :statman_server.add_subscriber(:statman_aggregator)
    :newrelic_poller.start_link(fn -> :statman_transformer.poll end)
    Logger.info "Started NewRelic Monitoring."

    {:ok, self}
  end
end

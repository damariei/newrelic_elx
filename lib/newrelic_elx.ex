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
    if !Application.get_env(:newrelic, :application_name) || !Application.get_env(:newrelic, :license_key) do
      Logger.warn "NewRelic not configured. Stopped Monitoring."
    else
      NewrelicElx.Sup.start_link()
      :statman_server.add_subscriber(:statman_aggregator)
      :newrelic_poller.start_link(fn -> :statman_transformer.poll end)
      Logger.info "Started NewRelic Monitoring."
    end

    {:ok, self}
  end
end

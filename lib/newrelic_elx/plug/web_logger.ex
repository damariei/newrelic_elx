defmodule NewRelicElx.Plug.WebLogger do
  @moduledoc """
  Custom Plug to automatically record all web requests as NewRelic transactions
  """

  import Plug.Conn

  def init(_) do
  end

  def call(conn, _opts) do
    transaction_name = "#{conn.request_path}##{conn.method}"

    conn
    |> put_private(:newrelic_transaction, NewrelicElx.Transaction.start(transaction_name))
    |> register_before_send(fn conn ->
        NewrelicElx.Transaction.finish(Map.get(conn.private, :newrelic_transaction))
        conn
        end)
  end
end

defmodule NewrelicElx.Poller do
  use GenServer

  def start_link(poll_fn) do
    start_link(poll_fn, &default_error_callback/2)
  end
  def start_link(poll_fn, error_callback) do
    GenServer.start_link(__MODULE__, [poll_fn, error_callback])
  end

  def init([poll_fn, error_callback]) do
    :erlang.send_after(60000, self, :poll)
    {:ok, %{poll_fn: poll_fn, error_callback: error_callback}}
  end

  def handle_info(:poll, state) do
    :erlang.send_after(60000, self, :poll)

    case state.poll_fn.() do
      {'EXIT', error} ->
        state.error_callback.(:poll_failed, error)
        :ok
      {metrics, errors} ->
        case NewrelicElx.Pusher.push(metrics, errors) do
          :ok ->
            :ok
          {:error, exception} ->
            state.error_callback.(:push_failed, exception)
        end
      _ ->
        :ok
    end

    {:noreply, state}
  end

  def default_error_callback(:poll_failed, error) do
    :error_logger.warning_msg("NewrelicElx.Poller: polling failed: ~p~n", [error])
  end
  def default_error_callback(:push_failed, error) do
    :error_logger.warning_msg("NewrelicElx.Poller: push failed: ~p~n", [error])
  end
end

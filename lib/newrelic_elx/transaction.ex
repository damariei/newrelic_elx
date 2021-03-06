defmodule NewrelicElx.Transaction do
  @moduledoc """
  Records a new web transaction in NewRelic from start() until finish() is called
  """

  defstruct [:name, :start_time]

  def start(name) when is_binary(name) do
    %__MODULE__{name: name, start_time: :os.timestamp}
  end

  def finish(%__MODULE__{start_time: start_time} = transaction) do
    end_time = :os.timestamp
    elapsed = :timer.now_diff(end_time, start_time)

    record_value!(transaction, :total, elapsed)
  end

  defp record_value!(%__MODULE__{name: name}, data, elapsed) do
    elapsed = round(elapsed / 1000.0) * 1000 # Round to milliseconds
    :ok = Exmetrics.Counter.incr({name, data, elapsed})
  end
end

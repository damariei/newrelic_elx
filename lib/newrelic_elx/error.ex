defmodule NewrelicElx.Error do
  @moduledoc """
  Records a error in NewRelic
  """

  @doc """
  path - web request path
  type - error type
  message - optional error message
  stack_trace - optional stack trace
  """
  def record(path, type, message \\ "", stack_trace \\ "") do
    :statman_counter.incr({path, {:error, {type, message, stack_trace}}})
  end
end

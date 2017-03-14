defmodule NewrelicElx.Pusher do

  def push(data, errors) do
    try do
      {:ok, hostname} = :inet.gethostname
      hostname = to_string(hostname)

      collector = get_redirect_host()
      run_id = connect(collector, hostname)

      case push_metric_data(collector, run_id, data) do
        :ok ->
          push_error_data(collector, run_id, errors)
        error ->
          error
      end
    catch
      exception ->
        {:error, exception}
    end
  end

  def push_metric_data(collector, run_id, data) do
    url = newrelic_url_format(collector, :metric_data, %{run_id: run_id})
    opts = [run_id,  :os.system_time(:seconds) - 60, :os.system_time(:seconds), data]

    push_data(url, opts)
  end

  def push_error_data(collector, run_id, data) do
    url = newrelic_url_format(collector, :error_data, %{run_id: run_id})
    opts = [run_id, data]

    push_data(url, opts)
  end

  def push_data(url, data) do
    case call_newrelic(url, :post, data) do
      %HTTPotion.Response{body: body, status_code: 200} ->
        case Poison.decode!(body)["exception"] do
          nil ->
            :ok
          exception ->
            {:error, exception}
        end
      _ ->
        throw(:newrelic_down)
    end
  end

  def get_redirect_host do
    url = newrelic_url_format("collector.newrelic.com", :get_redirect_host)
    case call_newrelic(url, :get) do
      %HTTPotion.Response{body: body, status_code: 200} ->
        Poison.decode!(body)["return_value"]
      _ ->
        throw(:newrelic_down)
    end
  end

  def connect(collector, hostname) do
    url = newrelic_url_format(collector, :connect)

    opts = [%{
      agent_version: "2.80.1.61",
      app_name: app_names(),
      host: hostname,
      identifier: full_app_name(),
      environment: [],
      pid: String.to_integer(System.get_pid),
      language: "python",
      settings: %{}
    }]

    case call_newrelic(url, :post, opts) do
      %HTTPotion.Response{body: body, status_code: 200} ->
        Poison.decode!(body)["return_value"]["agent_run_id"]
      _ ->
        throw(:newrelic_down)
    end
  end


  def newrelic_url_format(host, method, query_opts \\ %{}) do
    url = "http://#{host}/agent_listener/invoke_raw_method"
    opts = Map.merge(query_opts, %{
      protocol_version: 10,
      marshal_format: :json,
      method: method,
      license_key: license_key()
    })

    query_params =
      opts
      |> Enum.map(fn {k,v} -> "#{k}=#{v}" end)
      |> Enum.join("&")
    "#{url}?#{query_params}"
  end

  def call_newrelic(url, http_method, opts \\ %{}) do
    headers = ["Content-Encoding": "identity"]

    case http_method do
      :get ->
        HTTPotion.get(url, headers: headers)
      :post ->
        HTTPotion.post(url, body: :jiffy.encode(opts), headers: headers)
    end
  end

  def license_key do
    Application.get_env(:newrelic_elx, :license_key)
  end

  def full_app_name do
    Application.get_env(:newrelic_elx, :application_name)
  end

  def app_names do
    String.split(full_app_name(), ";")
  end
end

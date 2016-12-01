defmodule NewrelicElx.Mixfile do
  use Mix.Project

  def project do
    [ app: :newrelic_elx,
      name: "NewRelic Elixir",
      version: "0.2.1",
      elixir: "~> 1.2",
      erlc_paths: ["erl"],
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      description: "NewRelic monitoring and error tracking for Elixir",
      source_url: "https://github.com/damariei/newrelic_elx",
      homepage_url: "https://github.com/damariei/newrelic_elx",
      package: package,
      docs: [
        extras: ["README.md"],
        main: "readme",
      ]
    ]
  end

  def application do
    [ mod: { NewrelicElx, [] },
      applications: [ :logger, :newrelic ]
    ]
  end

  defp deps do
    [ {:newrelic, git: "https://github.com/wooga/newrelic-erlang.git"},
      {:plug, "~> 1.0"}
    ]
  end

  defp package do
    %{ licenses: ["BSD 3-Clause"],
       links: %{"Github" => "https://github.com/damariei/newrelic_elx"}
     }
  end
end

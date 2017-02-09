defmodule NewrelicElx.Mixfile do
  use Mix.Project

  def project do
    [ app: :newrelic_elx,
      name: "NewRelic Elixir",
      version: "0.3.0",
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
      applications: [ :logger, :exmetrics, :httpotion ]
    ]
  end

  defp deps do
    [ {:httpotion, "~> 3.0.2"},
      {:jiffy, "~> 0.14.11"},
      {:poison, "~> 3.0"},
      {:exmetrics, "~> 1.0"},
      {:hdr_histogram, git: "https://github.com/HdrHistogram/hdr_histogram_erl.git", tag: "0.2.6"},
      {:plug, "~> 1.0"}
    ]
  end

  defp package do
    %{ licenses: ["BSD 3-Clause"],
       links: %{"Github" => "https://github.com/damariei/newrelic_elx"}
     }
  end
end

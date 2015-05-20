defmodule Rebind.Mixfile do
  use Mix.Project

  def project do
    [app: :rebind,
     version: "0.1.3",
     elixir: "~> 1.0",
     description: "rebind parse transform for erlang",
     package: package,
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [{:parse_trans, github: "uwiger/parse_trans"}]
  end

  defp package do
    [files: ["src", "mix.exs", "README*"],
     contributors: ["Cameron Bytheway"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/camshaft/rebind"}]
  end
end

defmodule Rebind.Mixfile do
  use Mix.Project

  def project do
    [app: :rebind,
     version: "0.1.0",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [{:parse_trans, github: "uwiger/parse_trans"}]
  end
end

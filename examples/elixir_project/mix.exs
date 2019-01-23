defmodule ElixirProject.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixir_project,
      version: "0.1.0",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,      
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:eapa, git: "https://github.com/Vonmo/eapa.git", tag: "v0.1.2"}
    ]
  end
end

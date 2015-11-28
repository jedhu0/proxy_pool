defmodule ProxyPool.Mixfile do
  use Mix.Project

  def project do
    [app: :proxy_pool,
     version: "0.0.1",
     elixir: "~> 1.1",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    apps = [:logger, :ssdb, :hackney, :httpoison, :cowboy]
    mod = {:mod, {ProxyPool, []} }

    case Mix.env do
      :test ->
        [{:applications, apps}]
      _ ->
        [{:applications, apps}, mod]
    end
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      { :ssdb, git: "https://github.com/chashu-code/ssdb-client-elixir.git"},
      { :lager, git: "git://github.com/basho/lager.git", tag: "3.0.1", override: true},
      { :exlager, git: "https://github.com/khia/exlager.git"},
      { :cowboy, "~> 1.0.0"},
      { :httpoison, "~> 0.7.2"},
      { :poison, "~> 1.5"},
    ]
  end
end

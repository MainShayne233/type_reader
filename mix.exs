defmodule TypeReader.MixProject do
  use Mix.Project

  def project do
    [
      app: :type_reader,
      version: "0.0.2",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package()
    ]
  end

  def application do
    []
  end

  defp description, do: "Resolve Elixir types from typespecs"

  defp package do
    [
      name: "type_reader",
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/MainShayne233/type_reader"}
    ]
  end



  defp deps do
    [
      {:mix_test_watch, "~> 0.8", only: :dev, runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end
end

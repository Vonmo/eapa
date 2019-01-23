defmodule ElixirProjectTest do
  use ExUnit.Case
  require Logger
  doctest ElixirProject

  test "greets the world" do
    assert ElixirProject.hello() == :world
  end

  test "load eapa" do
    x = :eapa_int.with_val(6, "1280001.10345")
    "1280001.103" = :eapa_int.to_float(3, x)
    "1280001.103450" = :eapa_int.to_float(6, x)
  end

end

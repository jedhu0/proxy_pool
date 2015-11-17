require Lager
require IEx
defmodule ProxyPoolTest do
  use ExUnit.Case, async: true
  doctest ProxyPool
  @default_ip "127.0.0.1:8080"

  setup do
    SSDB.query ["qclear", "proxy_pool_test", @default_ip]
    SSDB.query ["qpush", "proxy_pool_test", @default_ip]

    {:ok, _} = ProxyPool.start_link
    # Lager.info "proxy_pool server ~s", proxy_pool
    :ok
  end
  #
  # test "spawns get_one when proxy_pool was blank" do
  #   SSDB.query ["qclear", "proxy_pool_test", @default_ip]
  #   IEx.pry
  #   assert ProxyPool.get_one == :no_proxy_data
  # end

  test "spawns get_one when proxy_pool was blank will return random_proxy" do
    assert ProxyPool.get_one == @default_ip
  end
end

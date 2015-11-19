require Lager

defmodule ProxyPoolWorkerTest do
  use ExUnit.Case, async: true
  doctest ProxyPoolWorker
  @default_ip "127.0.0.1:8080"

  setup do
    # should use some mock method
    SSDB.query ["qclear", Application.get_env(:proxy_pool, :ssdb_key)]
    SSDB.query ["qpush", Application.get_env(:proxy_pool, :ssdb_key), @default_ip]

    {:ok, _} = ProxyPoolWorker.start_link
    :ok
  end
  #
  # test "spawns random when proxy_pool was blank" do
  #   SSDB.query ["qclear", "proxy_pool_test", @default_ip]
  #   assert ProxyPoolWorker.random == :no_proxy_data
  # end

  test "spawns random when proxy_pool was blank will return random_proxy" do
    assert ProxyPoolWorker.random == @default_ip
  end

  test "spawns fail_notice will remove the wrong ip from avaliable_list" do
    assert ProxyPoolWorker.random == @default_ip
    ProxyPoolWorker.fail_notice(@default_ip)
    assert ProxyPoolWorker.random == :no_proxy_data
  end

  test "spawns query_proxy_pool method will return nil when no data" do
    SSDB.query ["qclear", Application.get_env(:proxy_pool, :ssdb_key), @default_ip]
    assert {:ok, nil} == ProxyPoolWorker.query_proxy_pool
  end

  test "spawns query_proxy_pool method will return all proxys" do
    SSDB.query ["qpush", Application.get_env(:proxy_pool, :ssdb_key), "127.0.0.1:8081"]
    {:ok, proxys} = ProxyPoolWorker.query_proxy_pool
    assert Set.size(proxys.avaliable) == 2
  end

  test "spawns update call will update the proxy_list when avaliable disjoint old invalid_list" do
    SSDB.query ["qpush", Application.get_env(:proxy_pool, :ssdb_key), "127.0.0.1:8082", "127.0.0.1:8083", "127.0.0.1:8084"]
    assert :ok == ProxyPoolWorker.update
    assert ProxyPoolWorker.random != :no_proxy_data
  end

  test "spawns update call when avaliable joint old invalid_list" do
    assert :ok == ProxyPoolWorker.fail_notice(@default_ip)
    SSDB.query ["qpush", Application.get_env(:proxy_pool, :ssdb_key), "127.0.0.1:8082", "127.0.0.1:8083"]
    assert :ok == ProxyPoolWorker.update
    assert ProxyPoolWorker.random != @default_ip
  end

  test "spawns check_invalid_list" do

  end
end

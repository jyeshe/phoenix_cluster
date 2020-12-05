defmodule Epmdless_dist do

  def listen(name) do
    :inet_tcp_dist.listen name
  end

  def select(node) do
    :inet_tcp_dist.select node
  end

  def accept(listen) do
    :inet_tcp_dist.accept listen
  end

  def accept_connection(accept_pid, socket, my_node, allowed, setup_time) do
    :inet_tcp_dist.accept_connection accept_pid, socket, my_node, allowed, setup_time
  end

  def setup(node, type, my_node, long_or_short_names, setup_time) do
    :inet_tcp_dist.setup node, type, my_node, long_or_short_names, setup_time
  end

  def close(listen) do
    :inet_tcp_dist.close listen
  end

  def childspecs do
    :inet_tcp_dist.childspecs
  end
end

defmodule Epmdless_epmd_client do
  # erl_distribution wants us to start a worker process. We don't need one, though.
  def start_link do
    :ignore
  end

  # As of Erlang/OTP 19.1, register_node/3 is used instead of
  # register_node/2, passing along the address family, 'inet_tcp' or
  # 'inet6_tcp'.  This makes no difference for our purposes.
  def register_node(name, port, _family) do
    register_node(name, port)
  end

  def register_node(_name, _port) do
    # This is where we would connect to epmd and tell it which port
    # we're listening on, but since we're epmd-less, we don't do that.

    # Need to return a "creation" number between 1 and 3.
    creation = :rand.uniform 100
    {:ok, creation}
  end

  def port_please(name, _ip) do
    port = System.get_env("BEAM_PORT")
    # OTP 23 protocol version is 6.
    version = 6
    {:port, port, version}
  end

  def names(_hostname) do
    # Since we don't have epmd, we don't really know what other nodes
    # there are.
    {:error, :address}
  end
end

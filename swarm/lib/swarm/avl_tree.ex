defmodule Swarm.AVL do
  alias Swarm.Node
  @type root :: Node.t()

  @spec insert(root, charlist()) :: root
  def insert(nil, address), do: %Node{address: address, height: 1}

  def insert(root=%Node{height: h, left: l, right: r, address: addr}, address) do
    root =
      cond do
        address < addr ->
          %Node{root | left: insert(l, address)}
        address > addr ->
          %Node{root | right: insert(r, address)}
        address == addr ->
          root
      end
    root = %Node{root | height: 1 + max(height(root.left), height(root.right))}
    balance = get_balance(root)
    cond do
      balance > 1 and address < root.left.address ->
        ll(root)
      balance < -1 and address > root.right.address ->
        rr(root)
      balance > 1 and address > root.left.address ->
        lr(root)
      balance < -1 and address < root.right.address ->
        rl(root)
      true ->
        root
    end
  end

  defp ll(root) do
    %Node{
      address: addr,
      left: %Node{
        address: l_addr,
        left: left_l,
        right: left_r,
        height: h_l,
      },
      height: h,
      right: right,
    } = root

    %Node{
      address: l_addr,
      left: left_l,
      height: h,
      right: %Node{
        address: addr,
        left: left_r,
        height: h-1,
        right: right
      }
    }
  end

  defp rr(root) do
    %Node{
      address: addr,
      left: left,
      height: h,
      right: %Node{
        address: addr_r,
        left: right_l,
        right: right_r,
        height: h_r,
      }
    } = root

    %Node{
      address: addr_r,
      left: %Node{
        address: addr,
        left: left,
        right: right_l,
        height: h-1,
      },
      height: h,
      right: right_r,
    }
  end

  defp lr(root) do
    %Node{
      address: addr,
      left: %Node{
        address: addr_l,
        left: left_l,
        right: %Node{
          address: addr_lr,
          left: lrl,
          right: lrr,
          height: h_lr
        },
        height: h_l,
      },
      height: h,
      right: right,
    } = root

    %Node{
      address: addr_lr,
      left: %Node{
        address: addr_l,
        left: left_l,
        right: lrl,
        height: h-1,
      },
      right: %Node{
        address: addr,
        left: lrr,
        right: right,
        height: h-1,
      },
      height: h,
    }
  end

  defp rl(root) do
    %Node{
      address: addr,
      left: left,
      right: %Node{
        address: addr_r,
        left: %Node{
          address: addr_rl,
          left: rll,
          right: rlr,
        },
        right: right_r,
      },
      height: h,
    } = root

    %Node{
      address: addr_rl,
      left: %Node{
        address: addr,
        left: left,
        right: rll,
        height: h-1,
      },
      right: %Node{
        address: addr_r,
        left: rlr,
        right: right_r,
        height: h-1,
      },
      height: h,
    }
  end

  @spec height(root) :: integer()
  def height(nil), do: 0
  def height(root), do: root.height

  @spec get_balance(root) :: integer()
  def get_balance(nil), do: 0
  def get_balance(root), do: height(root.left) - height(root.right)

  def inorder(f, nil), do: []
  def inorder(f, root=%Node{left: l, right: r, address: addr}) when is_function(f) do
    inorder(f, l)
    f.(addr)
    inorder(f, r)
  end

  def to_list([nil]), do: nil
  def to_list([]), do: []
  def to_list( [%Node{address: addr, left: l, right: r} | t]) do
    res =
      cond do
        l != nil and r != nil ->
          [addr | to_list(t ++ [l,r])]
        l != nil and r == nil ->
          [addr | to_list(t ++ [l])]
        r != nil and l == nil ->
          [addr | to_list(t ++ [r])]
        r == nil and l == nil ->
          [addr | to_list(t)]
      end
    res
  end

end

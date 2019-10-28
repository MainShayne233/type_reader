defmodule Client do
  defmodule A do
    alias Client.B, as: Bee
    @type i :: integer()
    @type t :: binary()
    @type c(lhs, rhs) :: C.t(lhs, rhs)
    @type identity(val) :: val
    @type rec_a(val) :: rec_b(val)
    @type rec_b(val) :: rec_c(val)
    @type rec_c(val) :: rec_d(val)
    @type rec_d(val) :: rec_b(val)

    @type jump(val) :: Bee.jump(val)

    @type simple_union :: integer() | number()

    @type keyword_wrap(val) :: keyword(val)
  end

  defmodule B do
    alias A, as: Ayy

    @type t :: atom() | Ayy.t()
    @type c(lhs, rhs) :: A.c(lhs, rhs)
    @type jump(val) :: val
  end

  defmodule C do
    @type t(lhs, rhs) :: B.c(lhs, rhs)
  end

  @type t(lhs, rhs) :: B.t() | C.t(lhs, rhs) | lhs | [rhs]

  require TypeReader

  def run do
    quote do
      Client.t(Client.A.i(), float())
    end
    |> TypeReader.type_from_quoted()
  end
end

quote do
  %RemoteType{
    module: Client,
    name: :t,
    bindings: [
      lhs: %RemoteType{
        module: Client.A,
        name: :i,
        args: []
      },
      rhs: %TerminalType{
        name: :float
      }
    ]
  }
end

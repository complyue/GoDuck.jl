
macro void(x)
  if Meta.isexpr(x, :function, 2) || Meta.isexpr(x, :(->), 2)
    x.args[2] = let body = x.args[2]
      quote
        try
          $body
        finally
          return nothing
        end
      end
    end
    return esc(x)
  else
    return quote
      $(esc(x))
      nothing
    end
  end
end

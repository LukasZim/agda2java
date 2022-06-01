interface Visitor {}

interface Agda {}

interface AgdaData extends Agda {
    Agda match(Visitor visitor);
}

interface AgdaFunction extends Agda{
    Visitor getFn();
}

class AgdaFunctionImpl implements AgdaFunction {
    private Visitor fn;
    public AgdaFunctionImpl(Visitor fn) {
        this.fn = fn;
    }
    public Visitor getFn() {
        return fn;
    }
}
class Main
{
  public static void main (String[] args)
  {
  }
  abstract static class Bool
  {
    abstract <T> T match (Visitor<T> visitor)
    ;
    interface Visitor <T>
    {
      T True ()
      ;
      T False ()
      ;
    }
    public Bool ()
    {
    }
  }
  static class True extends Bool
  {
    public True ()
    {
    }
    <T> T match (Visitor<T> visitor)
    {
      return visitor.True();
    }
  }
  static class False extends Bool
  {
    public False ()
    {
    }
    <T> T match (Visitor<T> visitor)
    {
      return visitor.False();
    }
  }
  public void not (Object d)
  {
    return b.match(new Bool.Visitor<>()
                   {
                     public Bool True ()
                     {
                       return new False();
                     }
                     public Bool False ()
                     {
                       return new True();
                     }
                   });
  }
  abstract static class Nat
  {
    abstract <T> T match (Visitor<T> visitor)
    ;
    interface Visitor <T>
    {
      T zero ()
      ;
      T suc ()
      ;
    }
    public Nat ()
    {
    }
  }
  static class zero extends Nat
  {
    public zero ()
    {
    }
    <T> T match (Visitor<T> visitor)
    {
      return visitor.zero();
    }
  }
  static class suc extends Nat
  {
    public suc ()
    {
    }
    <T> T match (Visitor<T> visitor)
    {
      return visitor.suc();
    }
  }
  public void halve (Object e)
  {
  }
  public void a ()
  {
  }
  {
    True b = new True();
  }
  {
    False c = new False();
  }
  public void tripleAnd (Object f, Object g, Object h)
  {
  }
}
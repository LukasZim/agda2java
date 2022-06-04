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
  interface BoolVisitor extends Visitor
  {
    Agda False ()
    ;
    Agda True ()
    ;
  }
  abstract static class Bool implements AgdaData
  {
  }
  static class False extends Bool
  {
    public False ()
    {
    }
    public Agda match (Visitor visitor)
    {
      return ((BoolVisitor) visitor).False();
    }
  }
  static class True extends Bool
  {
    public True ()
    {
    }
    public Agda match (Visitor visitor)
    {
      return ((BoolVisitor) visitor).True();
    }
  }
  interface NatVisitor extends Visitor
  {
    Agda suc (Agda arg1)
    ;
    Agda zero ()
    ;
  }
  abstract static class Nat implements AgdaData
  {
  }
  static class suc extends Nat
  {
    private final Agda arg1;
    public suc (Agda arg1)
    {
      this.arg1 = arg1;
    }
    public Agda match (Visitor visitor)
    {
      return ((NatVisitor) visitor).suc(arg1);
    }
  }
  static class zero extends Nat
  {
    public zero ()
    {
    }
    public Agda match (Visitor visitor)
    {
      return ((NatVisitor) visitor).zero();
    }
  }
}
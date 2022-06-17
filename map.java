interface Visitor {}

interface Agda {}

interface AgdaData extends Agda {
    Agda match(Visitor visitor);
}

interface AgdaFunction extends Agda{
    Visitor getFn();
}

interface AgdaLambda extends Agda {
    Agda run(Agda arg);
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
    map = (AgdaLambda) a -> (AgdaLambda) b -> (AgdaLambda) c -> (AgdaLambda) d -> {
                                                                                    return ((AgdaData) a).match(new ListVisitor()
                                                                                                                {
                                                                                                                  public Agda Empty ()
                                                                                                                  {
                                                                                                                    return d;
                                                                                                                  }
                                                                                                                  public Agda Succ (Agda e, Agda f)
                                                                                                                  {
                                                                                                                    return new Succ(c, runFunction(null, ((AgdaLambda) runFunction(null, ((AgdaLambda) runFunction(c, ((AgdaLambda) runFunction(f, ((AgdaLambda) map)))))))));
                                                                                                                  }
                                                                                                                });
                                                                                  };
    plusOne = (AgdaLambda) g -> {
                                  new suc(g);
                                };
    test = runFunction(null, ((AgdaLambda) runFunction(null, ((AgdaLambda) runFunction(plusOne, ((AgdaLambda) runFunction(new Succ(new zero(), new Succ(new suc(new zero()), new Succ(new suc(new suc(new zero())), new Empty()))), ((AgdaLambda) map))))))));
  }
  public static Agda runFunction (Agda arg, AgdaLambda l)
  {
    return l.run(arg);
  }
  private static Agda map, plusOne, test;
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
  interface ListVisitor extends Visitor
  {
    Agda Succ (Agda arg2, Agda arg1)
    ;
    Agda Empty ()
    ;
  }
  abstract static class List implements AgdaData
  {
  }
  static class Succ extends List
  {
    private final Agda arg2;
    private final Agda arg1;
    public Succ (Agda arg2, Agda arg1)
    {
      this.arg2 = arg2;
      this.arg1 = arg1;
    }
    public Agda match (Visitor visitor)
    {
      return ((ListVisitor) visitor).Succ(arg2, arg1);
    }
  }
  static class Empty extends List
  {
    public Empty ()
    {
    }
    public Agda match (Visitor visitor)
    {
      return ((ListVisitor) visitor).Empty();
    }
  }
}
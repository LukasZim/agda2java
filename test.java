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
    minusOne = (AgdaLambda) a -> {
                                   return ((AgdaData) a).match(new NatVisitor()
                                                               {
                                                                 public Agda zero ()
                                                                 {
                                                                   var b = new zero();
                                                                   return b;
                                                                 }
                                                                 public Agda suc (Agda d)
                                                                 {
                                                                   var c = new suc(d);
                                                                   return d;
                                                                 }
                                                               });
                                 };
    ans = runFunction(new suc(new suc(new zero())), ((AgdaLambda) minusOne));
    minus = (AgdaLambda) e -> (AgdaLambda) f -> {
                                                  return ((AgdaData) e).match(new NatVisitor()
                                                                              {
                                                                                public Agda zero ()
                                                                                {
                                                                                  var g = new zero();
                                                                                  return g;
                                                                                }
                                                                                public Agda suc (Agda i)
                                                                                {
                                                                                  var h = new suc(i);
                                                                                  return ((AgdaData) e).match(new NatVisitor()
                                                                                                              {
                                                                                                                public Agda zero ()
                                                                                                                {
                                                                                                                  var j = new zero();
                                                                                                                  return i;
                                                                                                                }
                                                                                                                public Agda suc (Agda l)
                                                                                                                {
                                                                                                                  var k = new suc(l);
                                                                                                                  return runFunction(k, ((AgdaLambda) runFunction(l, ((AgdaLambda) minus))));
                                                                                                                }
                                                                                                              });
                                                                                }
                                                                              });
                                                };
  }
  public static Agda runFunction (Agda arg, AgdaLambda l)
  {
    return l.run(arg);
  }
  private static Agda minusOne, ans, minus;
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
  interface VectorVisitor extends Visitor
  {
    Agda Next (Agda arg3, Agda arg2, Agda arg1)
    ;
    Agda Empty ()
    ;
  }
  abstract static class Vector implements AgdaData
  {
  }
  static class Next extends Vector
  {
    private final Agda arg3;
    private final Agda arg2;
    private final Agda arg1;
    public Next (Agda arg3, Agda arg2, Agda arg1)
    {
      this.arg3 = arg3;
      this.arg2 = arg2;
      this.arg1 = arg1;
    }
    public Agda match (Visitor visitor)
    {
      return ((VectorVisitor) visitor).Next(arg3, arg2, arg1);
    }
  }
  static class Empty extends Vector
  {
    public Empty ()
    {
    }
    public Agda match (Visitor visitor)
    {
      return ((VectorVisitor) visitor).Empty();
    }
  }
}
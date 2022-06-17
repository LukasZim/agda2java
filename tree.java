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
    plus = (AgdaLambda) a -> (AgdaLambda) b -> {
                                                 return ((AgdaData) b).match(new NatVisitor()
                                                                             {
                                                                               public Agda zero ()
                                                                               {
                                                                                 return b;
                                                                               }
                                                                               public Agda suc (Agda c)
                                                                               {
                                                                                 return new suc(runFunction(c, ((AgdaLambda) runFunction(b, ((AgdaLambda) plus)))));
                                                                               }
                                                                             });
                                               };
    gen = (AgdaLambda) d -> {
                              return ((AgdaData) d).match(new NatVisitor()
                                                          {
                                                            public Agda zero ()
                                                            {
                                                              return new Leaf(new suc(d));
                                                            }
                                                            public Agda suc (Agda e)
                                                            {
                                                              return new Node(runFunction(e, ((AgdaLambda) gen)), runFunction(e, ((AgdaLambda) gen)));
                                                            }
                                                          });
                            };
    sun = (AgdaLambda) f -> {
                              return ((AgdaData) f).match(new TreeVisitor()
                                                          {
                                                            public Agda Node (Agda g, Agda h)
                                                            {
                                                              return runFunction(runFunction(g, ((AgdaLambda) sun)), ((AgdaLambda) runFunction(runFunction(h, ((AgdaLambda) sun)), ((AgdaLambda) plus))));
                                                            }
                                                            public Agda Leaf (Agda i)
                                                            {
                                                              return new suc(new zero());
                                                            }
                                                          });
                            };
    n = new suc(new zero());
    res = runFunction(runFunction(n, ((AgdaLambda) gen)), ((AgdaLambda) sun));
  }
  public static Agda runFunction (Agda arg, AgdaLambda l)
  {
    return l.run(arg);
  }
  private static Agda plus, gen, sun, n, res;
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
  interface TreeVisitor extends Visitor
  {
    Agda Leaf (Agda arg1)
    ;
    Agda Node (Agda arg2, Agda arg1)
    ;
  }
  abstract static class Tree implements AgdaData
  {
  }
  static class Leaf extends Tree
  {
    private final Agda arg1;
    public Leaf (Agda arg1)
    {
      this.arg1 = arg1;
    }
    public Agda match (Visitor visitor)
    {
      return ((TreeVisitor) visitor).Leaf(arg1);
    }
  }
  static class Node extends Tree
  {
    private final Agda arg2;
    private final Agda arg1;
    public Node (Agda arg2, Agda arg1)
    {
      this.arg2 = arg2;
      this.arg1 = arg1;
    }
    public Agda match (Visitor visitor)
    {
      return ((TreeVisitor) visitor).Node(arg2, arg1);
    }
  }
}
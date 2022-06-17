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
    fib = (AgdaLambda) d -> {
                              return ((AgdaData) d).match(new NatVisitor()
                                                          {
                                                            public Agda zero ()
                                                            {
                                                              return d;
                                                            }
                                                            public Agda suc (Agda e)
                                                            {
                                                              return ((AgdaData) e).match(new NatVisitor()
                                                                                          {
                                                                                            public Agda zero ()
                                                                                            {
                                                                                              return d;
                                                                                            }
                                                                                            public Agda suc (Agda f)
                                                                                            {
                                                                                              return runFunction(runFunction(e, ((AgdaLambda) fib)), ((AgdaLambda) runFunction(runFunction(f, ((AgdaLambda) fib)), ((AgdaLambda) plus))));
                                                                                            }
                                                                                          });
                                                            }
                                                          });
                            };
    main = (AgdaLambda) g -> {
                               runFunction(g, ((AgdaLambda) fib));
                             };
  }
  public static Agda runFunction (Agda arg, AgdaLambda l)
  {
    return l.run(arg);
  }
  private static Agda plus, fib, main;
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
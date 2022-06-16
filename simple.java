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
    id = (AgdaLambda) a -> {
                             a;
                           };
    not = (AgdaLambda) b -> {
                              return ((AgdaData) b).match(new BoolVisitor()
                                                          {
                                                            public Agda True ()
                                                            {
                                                              return new False();
                                                            }
                                                            public Agda False ()
                                                            {
                                                              return new True();
                                                            }
                                                          });
                            };
    ite = (AgdaLambda) c -> (AgdaLambda) d -> (AgdaLambda) e -> (AgdaLambda) f -> {
                                                                                    return ((AgdaData) c).match(new BoolVisitor()
                                                                                                                {
                                                                                                                  public Agda True ()
                                                                                                                  {
                                                                                                                    return e;
                                                                                                                  }
                                                                                                                  public Agda False ()
                                                                                                                  {
                                                                                                                    return f;
                                                                                                                  }
                                                                                                                });
                                                                                  };
    one = new suc(new zero());
    two = new suc(one);
    three = new suc(two);
    pred = (AgdaLambda) g -> {
                               return ((AgdaData) g).match(new NatVisitor()
                                                           {
                                                             public Agda zero ()
                                                             {
                                                               return g;
                                                             }
                                                             public Agda suc (Agda h)
                                                             {
                                                               return h;
                                                             }
                                                           });
                             };
    plus = (AgdaLambda) i -> (AgdaLambda) j -> {
                                                 return ((AgdaData) i).match(new NatVisitor()
                                                                             {
                                                                               public Agda zero ()
                                                                               {
                                                                                 return j;
                                                                               }
                                                                               public Agda suc (Agda k)
                                                                               {
                                                                                 return new suc(runFunction(k, ((AgdaLambda) runFunction(j, ((AgdaLambda) plus)))));
                                                                               }
                                                                             });
                                               };
    twice = (AgdaLambda) l -> {
                                return ((AgdaData) l).match(new NatVisitor()
                                                            {
                                                              public Agda zero ()
                                                              {
                                                                return l;
                                                              }
                                                              public Agda suc (Agda m)
                                                              {
                                                                return new suc(new suc(runFunction(m, ((AgdaLambda) twice))));
                                                              }
                                                            });
                              };
    pow2 = (AgdaLambda) n -> {
                               return ((AgdaData) n).match(new NatVisitor()
                                                           {
                                                             public Agda zero ()
                                                             {
                                                               return new suc(n);
                                                             }
                                                             public Agda suc (Agda o)
                                                             {
                                                               return runFunction(runFunction(o, ((AgdaLambda) pow2)), ((AgdaLambda) twice));
                                                             }
                                                           });
                             };
    consume = (AgdaLambda) p -> {
                                  return ((AgdaData) p).match(new NatVisitor()
                                                              {
                                                                public Agda zero ()
                                                                {
                                                                  return p;
                                                                }
                                                                public Agda suc (Agda q)
                                                                {
                                                                  return runFunction(q, ((AgdaLambda) consume));
                                                                }
                                                              });
                                };
    test2 = runFunction(runFunction(runFunction(runFunction(runFunction(three, ((AgdaLambda) twice)), ((AgdaLambda) twice)), ((AgdaLambda) twice)), ((AgdaLambda) pow2)), ((AgdaLambda) consume));
    head = (AgdaLambda) r -> (AgdaLambda) s -> (AgdaLambda) t -> {
                                                                   return ((AgdaData) r).match(new VecVisitor()
                                                                                               {
                                                                                                 public Agda con (Agda u, Agda v, Agda w)
                                                                                                 {
                                                                                                   return v;
                                                                                                 }
                                                                                               });
                                                                 };
    tail = (AgdaLambda) x -> (AgdaLambda) y -> (AgdaLambda) z -> {
                                                                   return ((AgdaData) x).match(new VecVisitor()
                                                                                               {
                                                                                                 public Agda con (Agda a1, Agda b1, Agda c1)
                                                                                                 {
                                                                                                   return c1;
                                                                                                 }
                                                                                               });
                                                                 };
    map = (AgdaLambda) d1 -> (AgdaLambda) e1 -> (AgdaLambda) f1 -> (AgdaLambda) g1 -> (AgdaLambda) h1 -> {
                                                                                                           return ((AgdaData) d1).match(new VecVisitor()
                                                                                                                                        {
                                                                                                                                          public Agda nil ()
                                                                                                                                          {
                                                                                                                                            return h1;
                                                                                                                                          }
                                                                                                                                          public Agda con (Agda i1, Agda j1, Agda k1)
                                                                                                                                          {
                                                                                                                                            return new con(null, g1, runFunction(null, ((AgdaLambda) runFunction(null, ((AgdaLambda) runFunction(null, ((AgdaLambda) runFunction(g1, ((AgdaLambda) runFunction(k1, ((AgdaLambda) map)))))))))));
                                                                                                                                          }
                                                                                                                                        });
                                                                                                         };
    test3 = runFunction(null, ((AgdaLambda) runFunction(null, ((AgdaLambda) runFunction(runFunction(null, ((AgdaLambda) runFunction(null, ((AgdaLambda) runFunction(runFunction(null, ((AgdaLambda) runFunction(null, ((AgdaLambda) runFunction(null, ((AgdaLambda) runFunction(new suc(), ((AgdaLambda) runFunction(new con(null, new zero(), new con(null, new suc(new zero()), new con(null, new suc(new suc(new zero())), new nil()))), ((AgdaLambda) map)))))))))), ((AgdaLambda) tail)))))), ((AgdaLambda) head))))));
    asdf = new zero();
    test4 = asdf;
    fie = (AgdaLambda) l1 -> {
                               new suc(l1);
                             };
    foe = (AgdaLambda) m1 -> {
                               new suc(runFunction(m1, ((AgdaLambda) fie)));
                             };
    fun = runFunction(runFunction(new suc(new suc(new zero())), ((AgdaLambda) fie)), ((AgdaLambda) runFunction(runFunction(new suc(new suc(new zero())), ((AgdaLambda) foe)), ((AgdaLambda) plus))));
  }
  public static Agda runFunction (Agda arg, AgdaLambda l)
  {
    return l.run(arg);
  }
  private static Agda id, not, ite, one, two, three, pred, plus, twice, pow2, consume, test2, head, tail, map, test3, asdf, test4, fie, foe, fun;
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
  interface VecVisitor extends Visitor
  {
    Agda con (Agda arg3, Agda arg2, Agda arg1)
    ;
    Agda nil ()
    ;
  }
  abstract static class Vec implements AgdaData
  {
  }
  static class con extends Vec
  {
    private final Agda arg3;
    private final Agda arg2;
    private final Agda arg1;
    public con (Agda arg3, Agda arg2, Agda arg1)
    {
      this.arg3 = arg3;
      this.arg2 = arg2;
      this.arg1 = arg1;
    }
    public Agda match (Visitor visitor)
    {
      return ((VecVisitor) visitor).con(arg3, arg2, arg1);
    }
  }
  static class nil extends Vec
  {
    public nil ()
    {
    }
    public Agda match (Visitor visitor)
    {
      return ((VecVisitor) visitor).nil();
    }
  }
}
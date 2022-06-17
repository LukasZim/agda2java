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
    equals = (AgdaLambda) a -> (AgdaLambda) b -> {
                                                   return ((AgdaData) b).match(new NatVisitor()
                                                                               {
                                                                                 public Agda zero ()
                                                                                 {
                                                                                   return ((AgdaData) a).match(new NatVisitor()
                                                                                                               {
                                                                                                                 public Agda zero ()
                                                                                                                 {
                                                                                                                   return new True();
                                                                                                                 }
                                                                                                                 public Agda suc (Agda c)
                                                                                                                 {
                                                                                                                   return new False();
                                                                                                                 }
                                                                                                               });
                                                                                 }
                                                                                 public Agda suc (Agda d)
                                                                                 {
                                                                                   return ((AgdaData) b).match(new NatVisitor()
                                                                                                               {
                                                                                                                 public Agda zero ()
                                                                                                                 {
                                                                                                                   return new False();
                                                                                                                 }
                                                                                                                 public Agda suc (Agda e)
                                                                                                                 {
                                                                                                                   return runFunction(d, ((AgdaLambda) runFunction(e, ((AgdaLambda) equals))));
                                                                                                                 }
                                                                                                               });
                                                                                 }
                                                                               });
                                                 };
    plus = (AgdaLambda) f -> (AgdaLambda) g -> {
                                                 return ((AgdaData) g).match(new NatVisitor()
                                                                             {
                                                                               public Agda zero ()
                                                                               {
                                                                                 return g;
                                                                               }
                                                                               public Agda suc (Agda h)
                                                                               {
                                                                                 return new suc(runFunction(h, ((AgdaLambda) runFunction(g, ((AgdaLambda) plus)))));
                                                                               }
                                                                             });
                                               };
    minus = (AgdaLambda) i -> (AgdaLambda) j -> {
                                                  return ((AgdaData) j).match(new NatVisitor()
                                                                              {
                                                                                public Agda zero ()
                                                                                {
                                                                                  return runFunction(i, ((AgdaLambda) j));
                                                                                }
                                                                                public Agda suc (Agda k)
                                                                                {
                                                                                  return ((AgdaData) j).match(new NatVisitor()
                                                                                                              {
                                                                                                                public Agda zero ()
                                                                                                                {
                                                                                                                  return i;
                                                                                                                }
                                                                                                                public Agda suc (Agda l)
                                                                                                                {
                                                                                                                  return runFunction(k, ((AgdaLambda) runFunction(l, ((AgdaLambda) minus))));
                                                                                                                }
                                                                                                              });
                                                                                }
                                                                              });
                                                };
    mult = (AgdaLambda) m -> (AgdaLambda) n -> {
                                                 return ((AgdaData) n).match(new NatVisitor()
                                                                             {
                                                                               public Agda zero ()
                                                                               {
                                                                                 return m;
                                                                               }
                                                                               public Agda suc (Agda o)
                                                                               {
                                                                                 return runFunction(n, ((AgdaLambda) runFunction(runFunction(o, ((AgdaLambda) runFunction(n, ((AgdaLambda) mult)))), ((AgdaLambda) plus))));
                                                                               }
                                                                             });
                                               };
    zif = (AgdaLambda) p -> (AgdaLambda) q -> (AgdaLambda) r -> (AgdaLambda) s -> {
                                                                                    return ((AgdaData) r).match(new BoolVisitor()
                                                                                                                {
                                                                                                                  public Agda True ()
                                                                                                                  {
                                                                                                                    return r;
                                                                                                                  }
                                                                                                                  public Agda False ()
                                                                                                                  {
                                                                                                                    return s;
                                                                                                                  }
                                                                                                                });
                                                                                  };
    id = (AgdaLambda) t -> (AgdaLambda) u -> {
                                               u;
                                             };
    concat = (AgdaLambda) v -> (AgdaLambda) w -> (AgdaLambda) x -> {
                                                                     return ((AgdaData) w).match(new ListVisitor()
                                                                                                 {
                                                                                                   public Agda Empty ()
                                                                                                   {
                                                                                                     return x;
                                                                                                   }
                                                                                                   public Agda listsuc (Agda y, Agda z)
                                                                                                   {
                                                                                                     return new listsuc(y, runFunction(null, ((AgdaLambda) runFunction(z, ((AgdaLambda) runFunction(x, ((AgdaLambda) concat)))))));
                                                                                                   }
                                                                                                 });
                                                                   };
    range = (AgdaLambda) a1 -> (AgdaLambda) b1 -> {
                                                    runFunction(null, ((AgdaLambda) runFunction(null, ((AgdaLambda) runFunction(runFunction(new suc(b1), ((AgdaLambda) runFunction(a1, ((AgdaLambda) minus)))), ((AgdaLambda) runFunction(a1, ((AgdaLambda) go))))))));
                                                  };
    go = (AgdaLambda) c1 -> (AgdaLambda) d1 -> (AgdaLambda) e1 -> (AgdaLambda) f1 -> {
                                                                                       return ((AgdaData) d1).match(new NatVisitor()
                                                                                                                    {
                                                                                                                      public Agda zero ()
                                                                                                                      {
                                                                                                                        return new Empty();
                                                                                                                      }
                                                                                                                      public Agda suc (Agda g1)
                                                                                                                      {
                                                                                                                        return new listsuc(f1, runFunction(null, ((AgdaLambda) runFunction(null, ((AgdaLambda) runFunction(g1, ((AgdaLambda) runFunction(new suc(f1), ((AgdaLambda) go)))))))));
                                                                                                                      }
                                                                                                                    });
                                                                                     };
    bind1 = (AgdaLambda) h1 -> (AgdaLambda) i1 -> (AgdaLambda) j1 -> {
                                                                       return ((AgdaData) h1).match(new ListVisitor()
                                                                                                    {
                                                                                                      public Agda Empty ()
                                                                                                      {
                                                                                                        return j1;
                                                                                                      }
                                                                                                      public Agda listsuc (Agda k1, Agda l1)
                                                                                                      {
                                                                                                        return new listsuc(new triple(k1, h1, i1), runFunction(h1, ((AgdaLambda) runFunction(i1, ((AgdaLambda) runFunction(l1, ((AgdaLambda) bind1)))))));
                                                                                                      }
                                                                                                    });
                                                                     };
    bind2 = (AgdaLambda) m1 -> (AgdaLambda) n1 -> {
                                                    return ((AgdaData) m1).match(new ListVisitor()
                                                                                 {
                                                                                   public Agda Empty ()
                                                                                   {
                                                                                     return n1;
                                                                                   }
                                                                                   public Agda listsuc (Agda o1, Agda p1)
                                                                                   {
                                                                                     return runFunction(null, ((AgdaLambda) runFunction(runFunction(o1, ((AgdaLambda) runFunction(m1, ((AgdaLambda) runFunction(runFunction(new suc(new zero()), ((AgdaLambda) runFunction(o1, ((AgdaLambda) range)))), ((AgdaLambda) bind1)))))), ((AgdaLambda) runFunction(runFunction(m1, ((AgdaLambda) runFunction(p1, ((AgdaLambda) bind2)))), ((AgdaLambda) concat))))));
                                                                                   }
                                                                                 });
                                                  };
    bind3 = (AgdaLambda) q1 -> {
                                 return ((AgdaData) q1).match(new ListVisitor()
                                                              {
                                                                public Agda Empty ()
                                                                {
                                                                  return q1;
                                                                }
                                                                public Agda listsuc (Agda r1, Agda s1)
                                                                {
                                                                  return runFunction(null, ((AgdaLambda) runFunction(runFunction(r1, ((AgdaLambda) runFunction(runFunction(new suc(new zero()), ((AgdaLambda) runFunction(r1, ((AgdaLambda) range)))), ((AgdaLambda) bind2)))), ((AgdaLambda) runFunction(runFunction(s1, ((AgdaLambda) bind3)), ((AgdaLambda) concat))))));
                                                                }
                                                              });
                               };
    alltriples = (AgdaLambda) t1 -> {
                                      runFunction(runFunction(new suc(new zero()), ((AgdaLambda) runFunction(t1, ((AgdaLambda) range)))), ((AgdaLambda) bind3));
                                    };
    pythagorean = (AgdaLambda) u1 -> {
                                       return ((AgdaData) u1).match(new TripleVisitor()
                                                                    {
                                                                      public Agda triple (Agda v1, Agda w1, Agda x1)
                                                                      {
                                                                        return runFunction(runFunction(runFunction(v1, ((AgdaLambda) runFunction(v1, ((AgdaLambda) mult)))), ((AgdaLambda) runFunction(runFunction(w1, ((AgdaLambda) runFunction(w1, ((AgdaLambda) mult)))), ((AgdaLambda) plus)))), ((AgdaLambda) runFunction(runFunction(x1, ((AgdaLambda) runFunction(x1, ((AgdaLambda) mult)))), ((AgdaLambda) equals))));
                                                                      }
                                                                    });
                                     };
    filterP = (AgdaLambda) y1 -> {
                                   return ((AgdaData) y1).match(new ListVisitor()
                                                                {
                                                                  public Agda Empty ()
                                                                  {
                                                                    return y1;
                                                                  }
                                                                  public Agda listsuc (Agda z1, Agda a2)
                                                                  {
                                                                    return runFunction(null, ((AgdaLambda) runFunction(runFunction(z1, ((AgdaLambda) pythagorean)), ((AgdaLambda) runFunction(new listsuc(z1, runFunction(a2, ((AgdaLambda) filterP))), ((AgdaLambda) runFunction(runFunction(a2, ((AgdaLambda) filterP)), ((AgdaLambda) zif))))))));
                                                                  }
                                                                });
                                 };
    triples = (AgdaLambda) b2 -> {
                                   runFunction(runFunction(b2, ((AgdaLambda) alltriples)), ((AgdaLambda) filterP));
                                 };
    sumThree = (AgdaLambda) c2 -> {
                                    return ((AgdaData) c2).match(new TripleVisitor()
                                                                 {
                                                                   public Agda triple (Agda d2, Agda e2, Agda f2)
                                                                   {
                                                                     return runFunction(runFunction(d2, ((AgdaLambda) runFunction(e2, ((AgdaLambda) plus)))), ((AgdaLambda) runFunction(f2, ((AgdaLambda) plus))));
                                                                   }
                                                                 });
                                  };
    sumall = (AgdaLambda) g2 -> {
                                  return ((AgdaData) g2).match(new ListVisitor()
                                                               {
                                                                 public Agda Empty ()
                                                                 {
                                                                   return new zero();
                                                                 }
                                                                 public Agda listsuc (Agda h2, Agda i2)
                                                                 {
                                                                   return runFunction(runFunction(h2, ((AgdaLambda) sumThree)), ((AgdaLambda) runFunction(runFunction(i2, ((AgdaLambda) sumall)), ((AgdaLambda) plus))));
                                                                 }
                                                               });
                                };
    test1 = runFunction(runFunction(new zero(), ((AgdaLambda) triples)), ((AgdaLambda) sumall));
  }
  public static Agda runFunction (Agda arg, AgdaLambda l)
  {
    return l.run(arg);
  }
  private static Agda equals, plus, minus, mult, zif, id, concat, range, go, bind1, bind2, bind3, alltriples, pythagorean, filterP, triples, sumThree, sumall, test1;
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
    Agda listsuc (Agda arg2, Agda arg1)
    ;
    Agda Empty ()
    ;
  }
  abstract static class List implements AgdaData
  {
  }
  static class listsuc extends List
  {
    private final Agda arg2;
    private final Agda arg1;
    public listsuc (Agda arg2, Agda arg1)
    {
      this.arg2 = arg2;
      this.arg1 = arg1;
    }
    public Agda match (Visitor visitor)
    {
      return ((ListVisitor) visitor).listsuc(arg2, arg1);
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
  interface TripleVisitor extends Visitor
  {
    Agda triple (Agda arg3, Agda arg2, Agda arg1)
    ;
  }
  abstract static class Triple implements AgdaData
  {
  }
  static class triple extends Triple
  {
    private final Agda arg3;
    private final Agda arg2;
    private final Agda arg1;
    public triple (Agda arg3, Agda arg2, Agda arg1)
    {
      this.arg3 = arg3;
      this.arg2 = arg2;
      this.arg1 = arg1;
    }
    public Agda match (Visitor visitor)
    {
      return ((TripleVisitor) visitor).triple(arg3, arg2, arg1);
    }
  }
}
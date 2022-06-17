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
        Universe = null;
        f = (AgdaLambda) a -> (AgdaLambda) b -> {
            return ((AgdaData) b).match(new UVisitor()
            {
                public Agda natType ()
                {
                    return ((AgdaData) a).match(new NatVisitor()
                    {
                        public Agda zero ()
                        {
                            return a;
                        }
                        public Agda suc (Agda c)
                        {
                            return c;
                        }
                    });
                }
                public Agda boolType ()
                {
                    return ((AgdaData) a).match(new BoolVisitor()
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
                }
                public Agda listType (Agda c)
                {
                    return ((AgdaData) a).match(new ListVisitor()
                    {
                        public Agda Empty ()
                        {
                            return a;
                        }
                        public Agda Listsuc(Agda x, Agda xs)
                        {
                            return new Listsuc(
                                    runFunction(c, ((AgdaLambda) runFunction(x, ((AgdaLambda) f)))),
                                    runFunction(b, ((AgdaLambda) runFunction(xs, ((AgdaLambda) f)))));
                        }
                    });
                }
            });
        };
        test = runFunction(new natType(), ((AgdaLambda) runFunction(new zero(), ((AgdaLambda) f))));
        test2 = runFunction(new listType(new boolType()), ((AgdaLambda) runFunction(new Listsuc(new False(), new Empty()), ((AgdaLambda) f))));
        test3 = runFunction(new listType(new listType(new boolType())), ((AgdaLambda) runFunction(new Listsuc(new Listsuc(new False(), new Empty()), new Listsuc(new Empty(), new Listsuc(new Listsuc(new True(), new Empty()), new Empty()))), ((AgdaLambda) f))));
    }
    public static Agda runFunction (Agda arg, AgdaLambda l)
    {
        return l.run(arg);
    }
    private static Agda Universe, f, test, test2, test3;
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
        Agda Listsuc(Agda arg2, Agda arg1)
                ;
        Agda Empty ()
                ;
    }
    abstract static class List implements AgdaData
    {
    }
    static class Listsuc extends List
    {
        private final Agda arg2;
        private final Agda arg1;
        public Listsuc(Agda arg2, Agda arg1)
        {
            this.arg2 = arg2;
            this.arg1 = arg1;
        }
        public Agda match (Visitor visitor)
        {
            return ((ListVisitor) visitor).Listsuc(arg2, arg1);
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
    interface UVisitor extends Visitor
    {
        Agda listType (Agda arg1)
                ;
        Agda boolType ()
                ;
        Agda natType ()
                ;
    }
    abstract static class U implements AgdaData
    {
    }
    static class listType extends U
    {
        private final Agda arg1;
        public listType (Agda arg1)
        {
            this.arg1 = arg1;
        }
        public Agda match (Visitor visitor)
        {
            return ((UVisitor) visitor).listType(arg1);
        }
    }
    static class boolType extends U
    {
        public boolType ()
        {
        }
        public Agda match (Visitor visitor)
        {
            return ((UVisitor) visitor).boolType();
        }
    }
    static class natType extends U
    {
        public natType ()
        {
        }
        public Agda match (Visitor visitor)
        {
            return ((UVisitor) visitor).natType();
        }
    }
}
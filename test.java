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
  abstract static class Bool extends AgdaData
  {
  }
  interface NatVisitor extends Visitor
  {
    Agda suc (Agda arg1)
    ;
    Agda zero ()
    ;
  }
  abstract static class Nat extends AgdaData
  {
  }
}
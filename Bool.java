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
  }
  public static Agda runFunction (Agda arg, AgdaLambda l)
  {
    return l.run(arg);
  }
  private static Agda;
  interface BoolVisitor extends Visitor
  {
    Agda true ()
    ;
    Agda false ()
    ;
  }
  abstract static class Bool implements AgdaData
  {
  }
  static class true extends Bool
  {
    public true ()
    {
    }
    public Agda match (Visitor visitor)
    {
      return ((BoolVisitor) visitor).true();
    }
  }
  static class false extends Bool
  {
    public false ()
    {
    }
    public Agda match (Visitor visitor)
    {
      return ((BoolVisitor) visitor).false();
    }
  }
}
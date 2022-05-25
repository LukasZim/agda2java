class Main
{
  public static void main (String[] args)
  {
  }
  abstract static class Bool
  {
    public <T> T match (Visitor<T> visitor)
    ;
    interface Visitor <T>
    {
    }
    public Bool ()
    {
    }
  }
  class true extends Bool
  {
    public true ()
    {
    }
  }
  class false extends Bool
  {
    public false ()
    {
    }
  }
  public void not ()
  {
  }
  public void a ()
  {
  }
  {
    true b = new true();
  }
  {
    false c = new false();
  }
  public void tripleAnd ()
  {
  }
  public void weirdOp ()
  {
  }
}
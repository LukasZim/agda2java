class Main
{
  public static void main (String[] args)
  {
  }
  abstract static class Bool
  {
    abstract <T> T match (Visitor<T> visitor)
    ;
    interface Visitor <T>
    {
      T True ()
      ;
      T False ()
      ;
    }
    public Bool ()
    {
    }
  }
  static class True extends Bool
  {
    public True ()
    {
    }
    <T> T match (Visitor<T> visitor)
    {
      return visitor.True();
    }
  }
  static class False extends Bool
  {
    public False ()
    {
    }
    <T> T match (Visitor<T> visitor)
    {
      return visitor.False();
    }
  }
  public Bool not (Object a)
  {
    return b.match(new Bool.Visitor<>()
                   {
                     public Bool True ()
                     {
                       return new False();
                     }
                     public Bool False ()
                     {
                       return new True();
                     }
                   });
  }
}
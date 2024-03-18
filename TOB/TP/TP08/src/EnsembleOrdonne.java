public interface EnsembleOrdonne<T extends Comparable<T>> extends Ensemble<T>{
  
  public T plusPetitElement();
  
  public T justePlusGrandQue(T cell);

}

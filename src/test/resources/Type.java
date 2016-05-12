
public class Type<T> {
    List<String> a;
    A<B<C>> b;
    String[] f;
    public static void main(String args) {
    }

    public A<B<C>> getB() {
        return b;
    }

    public <T> T m(T a) {

    }
}

public class TypeBound<B extends A> {

}
class Pt2 {
    public int x;

    Pt2() {
        System.out.println("pt2 default");
    }

    Pt2(int x, int y) {
        this.x = x - 3;
        System.out.println("Pt2 with " + x + " and " + y);
    }
}

class Pt3 extends Pt2 {
    public int x;

    Pt3(int x, int y, int z) {
        super(x, y);
        this.x = x + 7;
        System.out.println("Pt3 with " + z);
    }
}

class Main {
    public static void main(String[] args) {
        var p3345 = new Pt3(3, 4, 5);
        var p3678 = new Pt3(6, 7, 8);
        System.out.println(p3345.x);
        System.out.println(p3678.x);
        System.out.println(((Pt2)p3345).x);
        System.out.println(((Pt2)p3678).x);
    }
}
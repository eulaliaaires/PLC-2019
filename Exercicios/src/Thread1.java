import java.util.Scanner;
public class Thread1 extends Thread{
	
	private int cont;
	private int limit;
	
	Thread1(int limit){
		this.limit = limit;
	}
	
	public void run() {
		for (cont = 0 ; cont < limit; cont++) {
			System.out.println(cont);
		}
	}

	
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Scanner in = new Scanner(System.in);
		int amountThread = in.nextInt();
		int limit = in.nextInt();
		Thread store [] = new Thread[amountThread];
		for(int a = 0; a < amountThread; a++) {
			Thread t = new Thread1(limit);
			store[a] = t;
		}
		for(int b = 0; b < store.length; b++) {
			store[b].start();
		}
		
		
	}

}

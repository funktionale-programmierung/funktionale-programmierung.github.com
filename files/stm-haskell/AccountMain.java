package com.factisresearch.blog;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class OrderedLock {
	private static int s_order;
	private static synchronized int getAndIncOrder() {
		int x = s_order;
		s_order++;
		return x;
	}
	private Lock m_lock;
	private int m_order;
	public OrderedLock() {
		m_lock = new ReentrantLock();
		m_order = getAndIncOrder();
	}
	public int getOrder() {
		return m_order;
	}
	public void acquire() {
		m_lock.lock();
	}
	public void release() {
		m_lock.unlock();
	}
	public static void acquireLocks(OrderedLock l1, OrderedLock l2) {
		if (l1.getOrder() < l2.getOrder()) {
			l1.acquire();
			l2.acquire();
		} else if (l1.getOrder() > l2.getOrder()) {
			l2.acquire();
			l1.acquire();
		}
	}
	public static void releaseLocks(OrderedLock l1, OrderedLock l2) {
		if (l1.getOrder() < l2.getOrder()) {
			l2.release();
			l1.release();
		} else if (l1.getOrder() > l2.getOrder()) {
			l1.release();
			l2.release();
		}
	}
	public static void acquireLocks(OrderedLock l1, OrderedLock l2, OrderedLock l3) {
		if (l1.getOrder() < l2.getOrder() && l2.getOrder() < l3.getOrder()) {
			l1.acquire();
			l2.acquire();
			l3.acquire();
		} else if (l1.getOrder() < l2.getOrder() && l2.getOrder() > l3.getOrder()) {
			l1.acquire();	
			l3.acquire();
			l2.acquire();
		} else if (l1.getOrder() > l2.getOrder() && l2.getOrder() < l3.getOrder()) {
			l2.acquire();
			l3.acquire();
			l1.acquire();
		} else if (l1.getOrder() > l2.getOrder() && l2.getOrder() > l3.getOrder()) {
			l3.acquire();
			l2.acquire();
			l1.acquire();
		}
	}
	public static void releaseLocks(OrderedLock l1, OrderedLock l2, OrderedLock l3) {
		if (l1.getOrder() < l2.getOrder() && l2.getOrder() < l3.getOrder()) {
			l3.release();
			l2.release();
			l1.release();
		} else if (l1.getOrder() < l2.getOrder() && l2.getOrder() > l3.getOrder()) {
			l2.release();
			l3.release();
			l1.release();
		} else if (l1.getOrder() > l2.getOrder() && l2.getOrder() < l3.getOrder()) {
			l1.release();	
			l3.release();
			l2.release();
		} else if (l1.getOrder() > l2.getOrder() && l2.getOrder() > l3.getOrder()) {
			l1.release();
			l2.release();
			l3.release();
		}
	}
}
class Account {
	private int m_amount;
	private OrderedLock m_lock;
	public Account() {
		m_lock = new OrderedLock();
	}
	public int getAmount() {
		return m_amount;
	}
	public synchronized void deposit(int i) {
		m_amount += i;
	}
	public void withdraw(int i) {
		deposit(-i);
	}	
	public OrderedLock getLock() {	
		return m_lock;
	}
}

public class AccountMain {

	public static void transfer(Account k1, Account k2, int amount) {
		try {
			OrderedLock.acquireLocks(k1.getLock(), k2.getLock());
			k1.withdraw(amount);
			k2.deposit(amount);
		} finally {
			OrderedLock.releaseLocks(k1.getLock(), k2.getLock());
		}
	}
	
	public static void splitTransfer(Account k1, Account k2, Account k3, int amount) {
		try {
			OrderedLock.acquireLocks(k1.getLock(), k2.getLock(), k3.getLock());
			k1.withdraw(2 * amount);
			k2.deposit(amount);
			k3.deposit(amount);
		} finally {
			OrderedLock.releaseLocks(k1.getLock(), k2.getLock(), k3.getLock());
		}
	}
	
	public static void doWork(Account k1, Account k2, Account k3) {
		for (int i = 0; i < 10000; i++) {
			splitTransfer(k1, k2, k3, 50);
			transfer(k2, k1, 50);
			transfer(k3, k1, 50);
		}
	}
	public static void main(String[] args) throws InterruptedException {
		final Account k1 = new Account();
		final Account k2 = new Account();
		final Account k3 = new Account();
		k1.deposit(100);
		int nThreads = 500;
		Thread[] threads = new Thread[nThreads];
		for (int i = 0; i < nThreads; i++) {
			Thread t = new Thread(new Runnable() {
				public void run() {
					doWork(k1, k2, k3);
				}
			});
			threads[i] = t;
			t.start();
		}
		for (int i = 0; i < nThreads; i++) {
			threads[i].join();
		}
		System.out.println("k1=" + k1.getAmount() + ", k2=" + k2.getAmount() + ", k3=" + k3.getAmount());
	}
}

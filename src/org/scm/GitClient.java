package org.scm;

public class GitClient {
    private static String scliFile = "org/scm/scli.clj";
    private static String scliNs = "org.scm.scli";

    private void loadInit(String addr, int port) {
        CloUtil.load(scliFile);
        CloUtil.call(scliNs, "init", addr, port);
    }

    public GitClient(String addr, int port) {
        loadInit(addr, port);
    }

    public void run() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                while (true) {
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
        }).start();
    }

    public static void main(String[] args) {
        String addr = "127.0.0.1";
        int port = 1234;
        new GitClient(addr, port).run();
    }
}

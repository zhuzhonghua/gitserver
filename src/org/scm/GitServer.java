package org.scm;

public class GitServer {
    private static String ssrvFile = "org/scm/ssrv.clj";
    private static String ssrvNs = "org.scm.ssrv";

    private static String scmdFile = "org/scm/scmd.clj";

    private void loadInit(int port, String gitpath) {
        CloUtil.load(ssrvFile);
        CloUtil.load(scmdFile);

        CloUtil.call(ssrvNs, "init", port, gitpath);
    }

    public GitServer(String path) {
        loadInit(1234, path);

        new NReplServer();
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
        String path = "xxx-\\.git";
        new GitServer(path).run();
    }
}

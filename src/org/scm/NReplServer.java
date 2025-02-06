package org.scm;

public class NReplServer {
    private static final String snreplFile = "org/scm/snrepl.clj";
    private static final String snreplNs = "org.scm.snrepl";

    public NReplServer() {
        CloUtil.load(snreplFile);

        CloUtil.call(snreplNs, "init");

        Runtime.getRuntime().addShutdownHook(new Thread(() -> CloUtil.call(snreplNs, "deinit")));
    }
}

package com.avlsi.tools.cosim.spec;

public interface VisitorInterface {
    void visitCoSimLevelSpec(CoSimLevelSpec coSimLevelSpec);
    void visitCoSimSpecList(CoSimSpecList coSimSpecList);
    void visitCoSimSpec(CoSimSpec coSimSpec);
    void visitInstSpecList(InstSpecList instSpecList);
    void visitInstSpec(InstSpec instSpec);
    void visitModeListLevelSpec(ModeListLevelSpec modeListLevelSpec);
    void visitModeList(ModeList modeList);
    void visitMode(Mode mode);
}

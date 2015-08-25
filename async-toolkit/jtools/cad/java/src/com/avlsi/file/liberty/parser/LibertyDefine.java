package com.avlsi.file.liberty.parser;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;

public class LibertyDefine extends LibertyObject {
    LibertyDefine(final si2ObjectIdT obj) {
        super(obj);
    }
    
    public String getName() {
        final int[] err = { SI2DR_NO_ERROR };
        final String result = liberty.si2drDefineGetName(obj, err);
        LibertyUtil.checkErr("si2drDefineGetName", err);
        return result;
    }

    public String getAllowedGroupName() {
        final int[] err = { SI2DR_NO_ERROR };
        final String result = liberty.si2drDefineGetAllowedGroupName(obj, err);
        LibertyUtil.checkErr("si2drDefineGetAllowedGroupName", err);
        return result;
    }

    public int getValueType() {
        final int[] err = { SI2DR_NO_ERROR };
        final int result = liberty.si2drDefineGetValueType(obj, err);
        LibertyUtil.checkErr("si2drDefineGetValueType", err);
        return result;
    }
}

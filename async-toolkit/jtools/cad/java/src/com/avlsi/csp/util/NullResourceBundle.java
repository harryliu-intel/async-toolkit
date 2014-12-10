package com.avlsi.csp.util;

import java.util.ResourceBundle;
import java.util.Enumeration;

public class NullResourceBundle extends ResourceBundle {
    public Enumeration getKeys() { return null; }
    protected Object handleGetObject(String key) { return null; }
}

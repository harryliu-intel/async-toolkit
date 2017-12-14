package com.fulcrummicro.util.properties;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PropConfig {
    
    final List<PropSetVal> sets;
    final List<String> uses;
    final Map<String, Property> opts;
    
    public PropConfig() {
        // Ordering is important
        sets = new ArrayList<PropSetVal>();
        // Ordering doesn't matter
        opts = new HashMap<String, Property> ();
        this.uses = new ArrayList<String>();
    }
    
    public void addUses(String name) {
        uses.add(name);
    }
    
    public List<String> getUses() {
        return uses;
    }    
    
    public PropConfig clone() {
        PropConfig clone = new PropConfig();
        clone.uses.addAll(uses);
        clone.sets.addAll(sets);
        clone.opts.putAll(opts);
        return clone;
    }
    
    public void addSet(String key, String value) {
        sets.add(new PropSetVal(key, value));
    }
    
    public List<PropSetVal> getSets() {
        return sets;
    }
    
    public void addOpt(String key, Property value) {
        opts.put(key, value);
    }
    
    public Map<String, Property> getOpts() {
        return opts;
    }
    
    public Map<String, String> getDefaultValues() {
        Map<String, String> values = new HashMap<String, String>();
        for (String key : opts.keySet()) {
            values.put(key, opts.get(key).value);
        }
        return values;
    }
    
    public String expandKey(String key) {
        String evaluatedKey = new String(key);
        SubstExpr m = new SubstExpr(evaluatedKey);
        
        while(m.canSubst()) {
            String newKey = m.evaluateSubst();
            String expandedKey = expandKey(newKey);
            String expandedValue = null;
            if (opts.containsKey(expandedKey)) {
                expandedValue = opts.get(expandedKey).value; 
                for (int i = sets.size()-1; i >= 0; i--) {
                    PropSetVal psv = sets.get(i);
                    if (psv.name.equals(expandedKey)) {
                        expandedValue = psv.value;
                        break;
                    }
                }
            } else {
                throw new ExpansionErrorException("Unable to resolve key " + expandedKey);
            }            

            /* expand the subkey and evaluate it */
            evaluatedKey = evaluatedKey.substring(0, m.start()) +
                    expandedValue +
                    evaluatedKey.substring(m.end());
            
            m = new SubstExpr(evaluatedKey);
            return evaluatedKey;            
        }
        return evaluatedKey;
    }

    public String expandValue(String value) {
        String expandedValue = new String(value);
        if (new SubstExpr(value).canSubst()) {
            SubstExpr m = new SubstExpr(expandedValue);
            
            while(m.canSubst()) {
                String key = m.evaluateSubst();
                String expandedKey = expandKey(key);
                String configValue;
                if (opts.containsKey(expandedKey)) {
                    configValue = opts.get(expandedKey).value;
                    for (int i = sets.size()-1; i >= 0; i--) {
                        PropSetVal psv = sets.get(i);                        
                        if (psv.name.equals(expandedKey)) {
                            configValue = psv.value;
                            break;
                        }
                    }
                } else {
                    throw new ExpansionErrorException("Unable to resolve key " + expandedKey);
                }
                /* expand the subkey and evaluate it */
                expandedValue = expandedValue.substring(0, m.start()) +
                        configValue + expandedValue.substring(m.end());
                m = new SubstExpr(expandedValue);
            }
        }
        return expandedValue;
    }
}

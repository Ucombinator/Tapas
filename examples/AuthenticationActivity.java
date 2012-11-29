package com.kingpetey.safepasswordapp;

import java.util.HashMap;

import android.app.Activity;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;

public class AuthenticationActivity extends Activity {
	
	private HashMap<String, String> shadow;
	private TextView result;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_authentication);
        result = (TextView)findViewById(R.id.result);
        shadow = new HashMap<String, String>(2);
        shadow.put("petey", "password");
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.activity_authentication, menu);
        return true;
    }
    
    public void authenticate(View v) {
    	String username = ((EditText)findViewById(R.id.usernameText)).getText().toString();
    	String password = ((EditText)findViewById(R.id.passwordText)).getText().toString();
    	String hash = shadow.get(username);
    	if (hash != null && hash.equals(hash(password))) {
    		// authenticated
    		result.setText(R.string.Win);
    	} else {
    		// did not authenticate
    		result.setText(R.string.Lose);
    	}
    }

	private Object hash(String string) {
		return string;
	}
}

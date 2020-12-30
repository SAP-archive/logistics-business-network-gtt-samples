package com.sap.gtt.v2.sample.sof.configuration;

import com.google.gson.annotations.SerializedName;

public class Destination {
    @SerializedName(value = "Name")
    private String name;
    @SerializedName(value = "Description")
    private String description;
    @SerializedName(value = "Type")
    private String type;
    @SerializedName(value = "URL")
    private String url;
    @SerializedName(value = "CloudConnectorLocationId")
    private String cloudConnectorLocationId;
    @SerializedName(value = "Authentication")
    private String authentication;
    @SerializedName(value = "ProxyType")
    private String proxyType;
    @SerializedName(value = "User")
    private String user;
    @SerializedName(value = "Password")
    private String password;

    private String clientId;
    private String tokenServiceUser;
    private String tokenServiceURL;
    private String clientSecret;
    private String tokenServicePassword;


    public String getName() {
        return name;
    }

    public String getClientId() {
        return clientId;
    }

    public String getTokenServiceUser() {
        return tokenServiceUser;
    }

    public String getTokenServiceURL() {
        return tokenServiceURL;
    }

    public String getClientSecret() {
        return clientSecret;
    }

    public String getTokenServicePassword() {
        return tokenServicePassword;
    }

    public String getDescription() {
        return description;
    }

    public String getType() {
        return type;
    }

    public String getUrl() {
        return url;
    }

    public String getCloudConnectorLocationId() {
        return cloudConnectorLocationId;
    }

    public String getAuthentication() {
        return authentication;
    }

    public String getProxyType() {
        return proxyType;
    }

    public String getUser() {
        return user;
    }

    public String getPassword() {
        return password;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}

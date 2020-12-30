package com.sap.gtt.v2.sample.sst.common.model;

import com.google.gson.annotations.SerializedName;

/**
 * @author Min Li
 */
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

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getCloudConnectorLocationId() {
        return cloudConnectorLocationId;
    }

    public void setCloudConnectorLocationId(String cloudConnectorLocationId) {
        this.cloudConnectorLocationId = cloudConnectorLocationId;
    }

    public String getAuthentication() {
        return authentication;
    }

    public void setAuthentication(String authentication) {
        this.authentication = authentication;
    }

    public String getProxyType() {
        return proxyType;
    }

    public void setProxyType(String proxyType) {
        this.proxyType = proxyType;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getClientId() {
        return clientId;
    }

    public void setClientId(String clientId) {
        this.clientId = clientId;
    }

    public String getTokenServiceUser() {
        return tokenServiceUser;
    }

    public void setTokenServiceUser(String tokenServiceUser) {
        this.tokenServiceUser = tokenServiceUser;
    }

    public String getTokenServiceURL() {
        return tokenServiceURL;
    }

    public void setTokenServiceURL(String tokenServiceURL) {
        this.tokenServiceURL = tokenServiceURL;
    }

    public String getClientSecret() {
        return clientSecret;
    }

    public void setClientSecret(String clientSecret) {
        this.clientSecret = clientSecret;
    }

    public String getTokenServicePassword() {
        return tokenServicePassword;
    }

    public void setTokenServicePassword(String tokenServicePassword) {
        this.tokenServicePassword = tokenServicePassword;
    }
}

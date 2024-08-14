package com.be.rebook.auth.oauth.info;

import java.util.Map;

public abstract class OAuthMemberInfo {
	protected Map<String, Object> attributes;

	public OAuthMemberInfo(Map<String, Object> attributes) {
		this.attributes = attributes;
	}

	public Map<String, Object> getAttributes() {
		return attributes;
	}

	public abstract String getNickname();

	public abstract String getEmail();

	public abstract String getImageUrl();

	public abstract String getRole();

	public abstract Long getProviderId();
}

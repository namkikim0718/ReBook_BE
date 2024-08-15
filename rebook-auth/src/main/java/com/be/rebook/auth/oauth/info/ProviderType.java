package com.be.rebook.auth.oauth.info;

import lombok.Getter;

@Getter
public enum ProviderType {
	KAKAO("kakao");
	// if needs more, add here

	private String key;

	ProviderType(String key) {
		this.key = key;
	}

	public String getKey() {
		return key;
	}

	public static ProviderType keyOf(String key) {
		for (ProviderType value : ProviderType.values()) {
			if (value.key.equalsIgnoreCase(key)) {
				return value;
			}
		}
		return null;
	}
}

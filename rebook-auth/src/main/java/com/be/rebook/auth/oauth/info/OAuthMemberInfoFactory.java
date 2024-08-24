package com.be.rebook.auth.oauth.info;

import java.util.Map;

import com.be.rebook.auth.oauth.info.impl.KaKaoOauthMemberInfo;

public class OAuthMemberInfoFactory {
	public static OAuthMemberInfo getOAuth2MemberInfo(ProviderType providerType, Map<String, Object> attributes) {
		switch (providerType) {
			case KAKAO:
				return new KaKaoOauthMemberInfo(attributes);
			default:
				throw new IllegalArgumentException("Invalid Provider Type.");
		}
	}
}

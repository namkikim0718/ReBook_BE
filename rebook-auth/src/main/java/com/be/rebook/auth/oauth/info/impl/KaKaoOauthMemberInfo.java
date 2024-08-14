package com.be.rebook.auth.oauth.info.impl;

import java.util.Map;

import com.be.rebook.auth.oauth.info.OAuthMemberInfo;

public class KaKaoOauthMemberInfo extends OAuthMemberInfo {

    public KaKaoOauthMemberInfo(Map<String, Object> attributes) {
        super(attributes);
    }

    @Override
    public String getNickname() {
        Map<String, Object> properties = (Map<String, Object>) attributes.get("properties");
        if (properties != null && properties.get("nickname") != null) {
            return properties.get("nickname").toString();
        } else {
            Map<String, Object> kakaoAccount = (Map<String, Object>) attributes.get("kakao_account");
            Map<String, Object> profile = (Map<String, Object>) kakaoAccount.get("profile");
            return profile != null ? profile.get("nickname").toString() : null;
        }
    }

    @Override
    public String getEmail() {
        Map<String, Object> kakaoAccount = (Map<String, Object>) attributes.get("kakao_account");
        return kakaoAccount.get("email").toString(); // 이메일은 필수 데이터 이므로 null 체크 생략
    }

    @Override
    public String getImageUrl() {
        Map<String, Object> kakaoAccount = (Map<String, Object>) attributes.get("kakao_account");
        if (kakaoAccount != null) {
            Map<String, Object> profile = (Map<String, Object>) kakaoAccount.get("profile");
            return profile != null ? profile.get("profile_image_url").toString() : null;
        }
        return null;
    }

    @Override
    public String getRole() {
        return "ROLE_USER";
    }

    @Override
    public Long getProviderId() {
        return Long.valueOf(attributes.get("id").toString());
    }
}

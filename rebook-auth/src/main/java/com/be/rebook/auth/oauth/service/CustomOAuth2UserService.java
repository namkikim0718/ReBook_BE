package com.be.rebook.auth.oauth.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;

import com.be.rebook.auth.dto.CustomUserDetails;
import com.be.rebook.auth.entity.Members;
import com.be.rebook.auth.oauth.info.OAuthMemberInfo;
import com.be.rebook.auth.oauth.info.OAuthMemberInfoFactory;
import com.be.rebook.auth.oauth.info.ProviderType;
import com.be.rebook.auth.repository.MembersRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    @Autowired
    private final MembersRepository membersRepository;

    @Override
    public OAuth2User loadUser(OAuth2UserRequest userRequest) throws OAuth2AuthenticationException {
        OAuth2User oAuth2User = super.loadUser(userRequest);

        ProviderType providerType = ProviderType.keyOf(userRequest.getClientRegistration().getRegistrationId());
        OAuthMemberInfo oAuthMemberInfo = OAuthMemberInfoFactory.getOAuth2MemberInfo(providerType,
                oAuth2User.getAttributes());

        Members savedMember = membersRepository.findByUsername(oAuthMemberInfo.getEmail()).orElse(null);

        if (savedMember == null) {
            savedMember = Members.builder()
                    .username(oAuthMemberInfo.getEmail())
                    .role(oAuthMemberInfo.getRole())
                    .build();
            savedMember = membersRepository.save(savedMember);
        }

        return new CustomUserDetails(savedMember, oAuthMemberInfo.getAttributes());
    }
}

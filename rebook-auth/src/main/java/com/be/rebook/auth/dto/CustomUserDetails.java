package com.be.rebook.auth.dto;

import com.be.rebook.auth.entity.Members;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.oauth2.core.user.OAuth2User;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class CustomUserDetails implements OAuth2User, UserDetails {
    private final Members members;
    private Map<String, Object> attributes;

    public CustomUserDetails(Members members) {
        this.members = members;
    }

    public CustomUserDetails(Members members, Map<String, Object> attributes) {
        this.members = members;
        this.attributes = attributes;
    }

    public String getRoleName() {
        return members.getRole();
    }

    // 역할 반환
    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        Collection<GrantedAuthority> collection = new ArrayList<>();
        collection.add(members::getRole);
        return collection;
    }

    @Override
    public String getPassword() {
        return members.getPassword();
    }

    @Override
    public String getUsername() {
        return members.getUsername();
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return true;
    }

    @Override
    public Map<String, Object> getAttributes() {
        return this.attributes;
    }

    @Override
    public String getName() {
        return this.members.getUsername();
    }
}

package com.be.rebook.common.argumentresolver.auth;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class MemberLoginInfo {

    private String username;

    private String role;

    public String toString() {
        return "MemberLoginInfo{" +
                "username=" + username +
                ", role=" + role +
                '}';
    }
}
